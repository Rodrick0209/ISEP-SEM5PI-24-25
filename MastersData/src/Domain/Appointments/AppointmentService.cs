using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using DDDSample1.Domain.Appointments;
using DDDSample1.Domain.AvailabilitySlots;
using DDDSample1.Domain.OperationRequest;
using DDDSample1.Domain.OperationRooms;
using DDDSample1.Domain.Patients;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.StaffMembers;
using DDDSample1.Domain.Utils;
using Microsoft.OpenApi.Models;
using Org.BouncyCastle.Crypto.Prng;

namespace DDDSample1.Domain.Appointments
{
    public class AppointmentService : IAppointmentService
    {
        private readonly IUnitOfWork _unitOfWork;
        private readonly IAppointmentRepository _appointmentRepository;

        private readonly IOperationRoomRepository _operationRoomRepository;
        private readonly IOperationRequestRepository _operationRequestRepository;
        private readonly IAvailabilitySlotsRepository _availabilitySlotsRepository;
        private readonly IPatientRepository _patientRepository;
        private readonly IStaffRepository _staffRepository;

        public AppointmentService(IUnitOfWork unitOfWork, IAppointmentRepository appointmentRepository, IOperationRoomRepository operationRoomRepository, IOperationRequestRepository operationRequestRepository, IPatientRepository patientRepository, IAvailabilitySlotsRepository availabilitySlotsRepository)
        {
            this._unitOfWork = unitOfWork;
            this._appointmentRepository = appointmentRepository;
            this._operationRoomRepository = operationRoomRepository;
            this._operationRequestRepository = operationRequestRepository;
            this._patientRepository = patientRepository;
            this._availabilitySlotsRepository = availabilitySlotsRepository;
        }


        public async Task<AppointmentDto> AddAsync(CreatingAppointmentDto appointmentDto)
        {


            var opRoom = await checkOperationRoomByNameAsync(appointmentDto.OperationRoomId, appointmentDto);
            var opRequest = await CheckOperationRequestAsync(new OperationRequestId(appointmentDto.OperationRequestId));



            var requestedDate = DateOnly.Parse(appointmentDto.AppointmentTimeSlotDtoDate); //"yyyy-MM-dd"
            var requestedStart = int.Parse(appointmentDto.AppointmentTimeSlotDtoTimeSlotStartMinute);
            var requestedEnd = int.Parse(appointmentDto.AppointmentTimeSlotDtoTimeSlotEndMinute);

            // Verifica se a sala está disponível usando o método do domínio
            if (!opRoom.IsAvailable(
                requestedDate,
                requestedStart,
                requestedEnd))
            {
                throw new Exception("The operation room is occupied during the requested time.");
            }




            if (!opRequest.IsAvailable(opRequest.status))
            {
                throw new Exception("The operation request is cancelled or was already accepted.");
            }

            foreach (StaffId staff in opRequest.getStaffAnesthesyPhase())
            {
                var availabilitySlot = await _availabilitySlotsRepository.GetByStaffIdAsync(staff.Value);
                if (!availabilitySlot.IsAvailable(requestedDate, requestedStart, requestedEnd))
                {
                    throw new Exception("The staff member is not available during the requested time.");
                }
            }

            foreach (StaffId staff in opRequest.getStaffSurgeryPhase())
            {
                var availabilitySlot = await _availabilitySlotsRepository.GetByStaffIdAsync(staff.Value);
                if (!availabilitySlot.IsAvailable(requestedDate, requestedStart, requestedEnd))
                {
                    throw new Exception("The staff member is not available during the requested time.");
                }
            }





            // Cria o agendamento
            var appointment = new Appointment(
                new AppointmentTimeSlot(
                    requestedDate,
                    new TimeSlot(requestedStart, requestedEnd)),
                new OperationRoomId(appointmentDto.OperationRoomId),
                new OperationRequestId(appointmentDto.OperationRequestId)
            );

            opRoom.Appointments.Add(appointment);
            opRequest.Accepted();

            // Atualiza o estado da sala no repositório
            // Salva o agendamento no repositório
            await _appointmentRepository.AddAsync(appointment);

            // Salva todas as mudanças no banco de dados
            await _unitOfWork.CommitAsync();

            return AppointmentMapper.ToDto(appointment);
        }


        public async Task<AppointmentDto> UpdateAsync(EditingAppointmentDto dto)
        {
            var app = await _appointmentRepository.GetByIdAsync(new AppointmentId(dto.Id));

            if (app == null)
            {
                throw new BusinessRuleValidationException("Appointment not found");
            }







            if (!string.IsNullOrWhiteSpace(dto.OperationRoomId))
            {
                var opRoom = await checkOperationRoomByNameForEditingAsync(dto.OperationRoomId, dto);
                if (!app.OperationRoomId.Value.Equals(dto.OperationRoomId))
                {
                    if (opRoom.IsAvailable(app.AppointmentTimeSlot.Date, app.AppointmentTimeSlot.TimeSlot.StartMinute, app.AppointmentTimeSlot.TimeSlot.EndMinute))
                    {
                        app.ChangeOperationRoomId(dto.OperationRoomId);
                    }
                    else
                    {
                        throw new BusinessRuleValidationException("Operation Room is not available");
                    }

                }
            }

            if (dto.OperationRequestTeamForAnesthesy != null && dto.OperationRequestTeamForAnesthesy.Count > 0)
            {
                var opRequest = await CheckOperationRequestAsync(app.OperationRequestId);

                var staffIds = dto.OperationRequestTeamForAnesthesy.Select(id => new StaffId(id)).ToList();

                foreach (var staffId in staffIds)
                {
                    // Verificar se o staff existe na base de dados
                    var staffExists = await _staffRepository.GetByIdAsync(staffId);
                    if (staffExists == null)
                    {
                        throw new BusinessRuleValidationException($"Staff member with ID {staffId.Value} does not exist.");
                    }

                    // Adicionar o staff à equipe de anestesia
                    opRequest.staffAssignedSurgery.addStaffAnesthesyPhase(staffId);
                }
            }

            if (dto.OperationRequestTeamForSurgery != null && dto.OperationRequestTeamForSurgery.Count > 0)
            {
                var opRequest = await CheckOperationRequestAsync(app.OperationRequestId);

                var staffIds = dto.OperationRequestTeamForSurgery.Select(id => new StaffId(id)).ToList();

                foreach (var staffId in staffIds)
                {
                    // Verificar se o staff existe na base de dados
                    var staffExists = await _staffRepository.GetByIdAsync(staffId);
                    if (staffExists == null)
                    {
                        throw new BusinessRuleValidationException($"Staff member with ID {staffId.Value} does not exist.");
                    }

                    // Adicionar o staff à equipe de cirurgia
                    opRequest.staffAssignedSurgery.addStaffSurgeryPhase(staffId);
                }
            }



            if (!string.IsNullOrWhiteSpace(dto.AppointmentStatus) && !app.AppointmentStatus.Equals(dto.AppointmentStatus))
            {
                if (dto.AppointmentStatus.Equals("Completed"))
                {
                    app.Completed();
                }
                else
                {
                    app.Cancelled();
                }

            }


            var newDate = !string.IsNullOrWhiteSpace(dto.AppointmentTimeSlotDtoDate)
                ? DateOnly.Parse(dto.AppointmentTimeSlotDtoDate)
                : app.AppointmentTimeSlot.Date; // Usa o valor atual como fallback

            var newStartMinute = !string.IsNullOrWhiteSpace(dto.AppointmentTimeSlotDtoTimeSlotStartMinute)
                ? int.Parse(dto.AppointmentTimeSlotDtoTimeSlotStartMinute)
                : app.AppointmentTimeSlot.TimeSlot.StartMinute; // Usa o valor atual como fallback

            var newEndMinute = !string.IsNullOrWhiteSpace(dto.AppointmentTimeSlotDtoTimeSlotEndMinute)
                ? int.Parse(dto.AppointmentTimeSlotDtoTimeSlotEndMinute)
                : app.AppointmentTimeSlot.TimeSlot.EndMinute; // Usa o valor atual como fallback

            // Verifica se é necessário atualizar
            if (!app.AppointmentTimeSlot.Date.Equals(newDate) ||
                !app.AppointmentTimeSlot.TimeSlot.StartMinute.Equals(newStartMinute) ||
                !app.AppointmentTimeSlot.TimeSlot.EndMinute.Equals(newEndMinute))
            {
                // Valida a disponibilidade da sala de operações
                var opRoom = await _operationRoomRepository.GetByIdAsync(app.OperationRoomId);
                if (!opRoom.IsAvailable(newDate, newStartMinute, newEndMinute))
                {
                    throw new BusinessRuleValidationException("Operation Room is not available for the specified date and/or time slot.");
                }

                // Obtém os membros do staff atribuídos à operação
                var opRequest = await _operationRequestRepository.GetByIdAsync(app.OperationRequestId);


                // Valida a disponibilidade de cada membro do staff
                foreach (StaffId staff in opRequest.getStaffAnesthesyPhase())
                {
                    var availabilitySlot = await _availabilitySlotsRepository.GetByStaffIdAsync(staff.Value);
                    if (!availabilitySlot.IsAvailable(newDate, newStartMinute, newEndMinute))
                    {
                        throw new Exception("The staff member is not available during the requested time, please update it.");
                    }
                }

                foreach (StaffId staff in opRequest.getStaffSurgeryPhase())
                {
                    var availabilitySlot = await _availabilitySlotsRepository.GetByStaffIdAsync(staff.Value);
                    if (!availabilitySlot.IsAvailable(newDate, newStartMinute, newEndMinute))
                    {
                        throw new Exception("The staff member is not available during the requested time, please update it.");
                    }
                }

                // Aplica as alterações
                if (!app.AppointmentTimeSlot.Date.Equals(newDate))
                {
                    app.AppointmentTimeSlot.ChangeDate(newDate);
                }

                if (!app.AppointmentTimeSlot.TimeSlot.StartMinute.Equals(newStartMinute) ||
                    !app.AppointmentTimeSlot.TimeSlot.EndMinute.Equals(newEndMinute))
                {
                    app.AppointmentTimeSlot.ChangeTimeSlot(newStartMinute, newEndMinute);
                }
            }


            await _unitOfWork.CommitAsync();


            return AppointmentMapper.ToDto(app);

        }



        public async Task<List<Appointment>> GetAllAsync()
        {
            return await this._appointmentRepository.GetAllAsync();
        }


        public async Task<OperationRoom> checkOperationRoomByNameAsync(string operationRoom, CreatingAppointmentDto appointment)
        {

            try
            {
                var spec = await this._operationRoomRepository.GetByNameAsync(operationRoom);

                if (spec == null)
                {
                    throw new BusinessRuleValidationException("Operation Room not found");
                }
                appointment.OperationRoomId = spec.Id.AsString();
                return spec;
            }
            catch (Exception e)
            {
                throw new BusinessRuleValidationException("Operation room not Found");
            }
        }



        public async Task<OperationRoom> checkOperationRoomByNameForEditingAsync(string operationRoom, EditingAppointmentDto appointment)
        {

            try
            {
                var spec = await this._operationRoomRepository.GetByNameAsync(operationRoom);

                if (spec == null)
                {
                    throw new BusinessRuleValidationException("Operation Room not found");
                }
                appointment.OperationRoomId = spec.Id.AsString();
                return spec;
            }
            catch (Exception e)
            {
                throw new BusinessRuleValidationException("Operation room not Found");
            }
        }


        private async Task<DDDSample1.Domain.OperationRequest.OperationRequest> CheckOperationRequestAsync(OperationRequestId id)
        {
            var opr = await this._operationRequestRepository.GetByIdAsync(id);
            if (opr == null)
                throw new BusinessRuleValidationException("Operation Request not found");
            return opr;
        }



        public async Task<AppointmentDtoUI> GetByIdAsync(AppointmentId id)
        {

            var app = await _appointmentRepository.GetByIdAsync(id);
            var opRoomNumber = _operationRoomRepository.GetByIdAsync(app.OperationRoomId).Result.RoomNumber.roomNumber;
            var opRequest = _operationRequestRepository.GetByIdAsync(app.OperationRequestId).Result;
            var patientMedicalRecordNumber = _patientRepository.GetByIdAsync(new PatientId(opRequest.patientId)).Result.MedicalRecordNumber._medicalRecordNumber;
            var priority = opRequest.priority.priority;

            return app == null ? null : AppointmentMapper.ToDtoUI(app, priority, patientMedicalRecordNumber, opRoomNumber);

        }


        public async Task<List<AppointmentDtoUI>> GetAllForUIAsync()
        {
            List<Appointment> appointments = await this._appointmentRepository.GetAllAsync();
            List<AppointmentDtoUI> appointmentDtos = new List<AppointmentDtoUI>();
            foreach (Appointment app in appointments)
            {
                var opRoomNumber = _operationRoomRepository.GetByIdAsync(app.OperationRoomId).Result.RoomNumber.roomNumber;
                var opRequest = _operationRequestRepository.GetByIdAsync(app.OperationRequestId).Result;
                var patientMedicalRecordNumber = _patientRepository.GetByIdAsync(new PatientId(opRequest.patientId)).Result.MedicalRecordNumber._medicalRecordNumber;
                var priority = opRequest.priority.priority;
                appointmentDtos.Add(AppointmentMapper.ToDtoUI(app, priority, patientMedicalRecordNumber, opRoomNumber));
            }
            return appointmentDtos;
        }





    }
}


