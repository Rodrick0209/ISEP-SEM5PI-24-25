using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using DDDSample1.Domain.Appointments;
using DDDSample1.Domain.OperationRequest;
using DDDSample1.Domain.OperationRooms;
using DDDSample1.Domain.Patients;
using DDDSample1.Domain.Shared;
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
        private readonly IPatientRepository _patientRepository;

        public AppointmentService(IUnitOfWork unitOfWork, IAppointmentRepository appointmentRepository, IOperationRoomRepository operationRoomRepository, IOperationRequestRepository operationRequestRepository, IPatientRepository patientRepository)
        {
            this._unitOfWork = unitOfWork;
            this._appointmentRepository = appointmentRepository;
            this._operationRoomRepository = operationRoomRepository;
            this._operationRequestRepository = operationRequestRepository;
            this._patientRepository = patientRepository;
        }


        public async Task<AppointmentDto> AddAsync(CreatingAppointmentDto appointmentDto)
        {


            var opRoom = await checkOperationRoomByNameAsync(appointmentDto.OperationRoomId, appointmentDto);
            var opRequest = await CheckOperationRequestAsync(new OperationRequestId(appointmentDto.OperationRequestId));



            // Verifica se o horário está disponível
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


            // Salva o agendamento no repositório
            await _appointmentRepository.AddAsync(appointment);
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

            if (!string.IsNullOrWhiteSpace(dto.OperationRequestId))
            {
                var opRequest = await CheckOperationRequestAsync(new OperationRequestId(dto.OperationRequestId));

                // Verifica se o OperationRequest mudou
                if (!app.OperationRequestId.Value.Equals(dto.OperationRequestId))
                {
                    // Verifica disponibilidade do OperationRequest
                    if (!opRequest.IsAvailable(opRequest.status))
                    {
                        throw new BusinessRuleValidationException("Operation Request is not available");
                    }

                    // Verifica disponibilidade da OperationRoom
                    var opRoom = await _operationRoomRepository.GetByIdAsync(app.OperationRoomId);
                    if (!opRoom.IsAvailable(app.AppointmentTimeSlot.Date, app.AppointmentTimeSlot.TimeSlot.StartMinute, app.AppointmentTimeSlot.TimeSlot.EndMinute))
                    {
                        throw new BusinessRuleValidationException("Operation Room is not available");
                    }

                    // Atualiza o OperationRequestId
                    app.ChangeOperationRequestId(dto.OperationRequestId);
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
            : (DateOnly?)null;

            var newStartMinute = !string.IsNullOrWhiteSpace(dto.AppointmentTimeSlotDtoTimeSlotStartMinute)
                ? int.Parse(dto.AppointmentTimeSlotDtoTimeSlotStartMinute)
                : (int?)null;

            var newEndMinute = !string.IsNullOrWhiteSpace(dto.AppointmentTimeSlotDtoTimeSlotEndMinute)
                ? int.Parse(dto.AppointmentTimeSlotDtoTimeSlotEndMinute)
                : (int?)null;

            // Check if updates are needed
            if ((newDate != null && !app.AppointmentTimeSlot.Date.Equals(newDate)) ||
                (newStartMinute != null && !app.AppointmentTimeSlot.TimeSlot.StartMinute.Equals(newStartMinute)) ||
                (newEndMinute != null && !app.AppointmentTimeSlot.TimeSlot.EndMinute.Equals(newEndMinute)))
            {
                var opRoom = await _operationRoomRepository.GetByIdAsync(app.OperationRoomId);

                // Validate availability using the values from dto
                if (!opRoom.IsAvailable(newDate.Value, newStartMinute.Value, newEndMinute.Value))
                {
                    throw new BusinessRuleValidationException("Operation Room is not available for the specified date and/or time slot.");
                }

                // Apply changes
                if (newDate != null)
                {
                    app.AppointmentTimeSlot.ChangeDate(newDate.Value);
                }

                if (newStartMinute != null && newEndMinute != null)
                {
                    app.AppointmentTimeSlot.ChangeTimeSlot(newStartMinute.Value, newEndMinute.Value);
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


