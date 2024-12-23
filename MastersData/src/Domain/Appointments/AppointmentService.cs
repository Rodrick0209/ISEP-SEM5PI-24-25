using System;
using System.Collections.Generic;
using System.Diagnostics.CodeAnalysis;
using System.Linq;
using System.Threading.Tasks;
using DDDSample1.Domain.Appointments;
using DDDSample1.Domain.AvailabilitySlots;
using DDDSample1.Domain.OperationRequest;
using DDDSample1.Domain.OperationRooms;
using DDDSample1.Domain.OperationTypes;
using DDDSample1.Domain.Patients;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.StaffMembers;
using DDDSample1.Domain.Utils;

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

        private readonly IOperationTypeRepository _operationTypeRepository;

        public AppointmentService(IUnitOfWork unitOfWork, IAppointmentRepository appointmentRepository, IOperationRoomRepository operationRoomRepository, IOperationRequestRepository operationRequestRepository, IPatientRepository patientRepository, IAvailabilitySlotsRepository availabilitySlotsRepository, IStaffRepository staffRepository, IOperationTypeRepository operationTypeRepository)
        {
            this._unitOfWork = unitOfWork;
            this._appointmentRepository = appointmentRepository;
            this._operationRoomRepository = operationRoomRepository;
            this._operationRequestRepository = operationRequestRepository;
            this._patientRepository = patientRepository;
            this._availabilitySlotsRepository = availabilitySlotsRepository;
            this._staffRepository = staffRepository;
            this._operationTypeRepository = operationTypeRepository;
        }


        public async Task<AppointmentDto> AddAsync(CreatingAppointmentDto appointmentDto)
        {


            var opRoom = await checkOperationRoomByNameAsync(appointmentDto.OperationRoomId, appointmentDto);
            var opRequest = await CheckOperationRequestAsync(new OperationRequestId(appointmentDto.OperationRequestId));


            var requestedDate = DateOnly.Parse(appointmentDto.AppointmentTimeSlotDtoDate);
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
                opRequest.staffAssignedSurgery.addStaffAnesthesyPhase(staff);
            }

            foreach (StaffId staff in opRequest.getStaffSurgeryPhase())
            {
                var availabilitySlot = await _availabilitySlotsRepository.GetByStaffIdAsync(staff.Value);
                if (!availabilitySlot.IsAvailable(requestedDate, requestedStart, requestedEnd))
                {
                    throw new Exception("The staff member is not available during the requested time.");
                }
                opRequest.staffAssignedSurgery.addStaffSurgeryPhase(staff);
            }


            var appointment = new Appointment(
                new AppointmentTimeSlot(
                    requestedDate,
                    new TimeSlot(requestedStart, requestedEnd)),
                new OperationRoomId(appointmentDto.OperationRoomId),
                new OperationRequestId(appointmentDto.OperationRequestId)
            );

            opRoom.Appointments.Add(appointment);
            opRequest.Accepted();

            await _appointmentRepository.AddAsync(appointment);

            // Salva todas as mudanças no banco de dados
            await _unitOfWork.CommitAsync();

            return AppointmentMapper.ToDto(appointment);
        }


        public async Task<EditingAppointmentDto> UpdateAsync(EditingAppointmentDto dto)
        {
            var app = await _appointmentRepository.GetByIdAsync(new AppointmentId(dto.Id));

            if (app == null)
            {
                throw new BusinessRuleValidationException("Appointment not found");
            }


            if (!string.IsNullOrWhiteSpace(dto.OperationRoomId))
            {
                var opRoom = await checkOperationRoomByNameForEditingAsync(dto.OperationRoomId, dto);

                if (!app.OperationRoomId.Value.Equals(opRoom.Id.Value))
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


            var anesthesiaStaffIds = new List<string>();
            if (dto.anesthesiaStaff != null && dto.anesthesiaStaff.Count > 0)
            {
                var opRequest = await CheckOperationRequestAsync(app.OperationRequestId);

                foreach (var staffId in dto.anesthesiaStaff)
                {
                    // Verificar se o staff existe na base de dados
                    var staffExists = await _staffRepository.GetByIdsAsync(staffId);

                    // Adicionar o staff à equipe de anestesia
                    opRequest.staffAssignedSurgery.addStaffAnesthesyPhase(staffExists.Id);
                    anesthesiaStaffIds.Add(staffId);
                }
            }


            var surgeryStaffIds = new List<string>();
            if (dto.surgeryStaff != null && dto.surgeryStaff.Count > 0)
            {
                var opRequest = await CheckOperationRequestAsync(app.OperationRequestId);


                foreach (var staffId in dto.surgeryStaff)
                {

                    // Verificar se o staff existe na base de dados
                    var staffExists = await _staffRepository.GetByIdsAsync(staffId);

                    // Adicionar o staff à equipe de anestesia
                    opRequest.staffAssignedSurgery.addStaffSurgeryPhase(staffExists.Id);
                    surgeryStaffIds.Add(staffId);

                }
            }


            var newDate = !string.IsNullOrWhiteSpace(dto.appointmentTimeSlot.date)
                ? DateOnly.Parse(dto.appointmentTimeSlot.date)
                : app.AppointmentTimeSlot.Date; // Usa o valor atual como fallback

            var newStartMinute = !string.IsNullOrWhiteSpace(dto.appointmentTimeSlot.timeSlot.StartTime)
                ? int.Parse(dto.appointmentTimeSlot.timeSlot.StartTime)
                : app.AppointmentTimeSlot.TimeSlot.StartMinute; // Usa o valor atual como fallback

            var newEndMinute = !string.IsNullOrWhiteSpace(dto.appointmentTimeSlot.timeSlot.EndTime)
                ? int.Parse(dto.appointmentTimeSlot.timeSlot.EndTime)
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

                    if (availabilitySlot != null)
                    {
                        if (!availabilitySlot.IsAvailable(newDate, newStartMinute, newEndMinute))
                        {
                            throw new Exception("1The staff member is not available during the requested time, please update it.");
                        }
                    }
                }


                foreach (StaffId staff in opRequest.getStaffSurgeryPhase())
                {
                    var availabilitySlot = await _availabilitySlotsRepository.GetByStaffIdAsync(staff.Value);

                    
                    Console.WriteLine($"newDate: {newDate}, newStartMinute: {newStartMinute}, newEndMinute: {newEndMinute}");

                    if (availabilitySlot != null)
                    {
                        if (!availabilitySlot.IsAvailable(newDate, newStartMinute, newEndMinute))
                        {
                            throw new Exception("2The staff member is not available during the requested time, please update it.");
                        }
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

            

            return AppointmentMapper.ToEditingDto(app,surgeryStaffIds,anesthesiaStaffIds);

        }



        public async Task<List<Appointment>> GetAllAsync()
        {
            return await this._appointmentRepository.GetAllAsync();
        }


        public async Task<OperationRoom> checkOperationRoomByRodrigo(CreateAppointmentWithMedicalTeam appointmentDto)
        {
            try
            {
                var spec = await this._operationRoomRepository.GetByIdAsync(new OperationRoomId(appointmentDto.OperationRoomId));

                if (spec == null)
                {
                    throw new BusinessRuleValidationException("Operation Room not found");
                }
                return spec;
            }
            catch (Exception e)
            {
                throw new BusinessRuleValidationException("Operation room not Found");


            }
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
            var surgery = opRequest.staffAssignedSurgery;

            return app == null ? null : AppointmentMapper.ToDtoUI(app, priority, patientMedicalRecordNumber, opRoomNumber, surgery.staffSurgeryPhase.Select(s => s.Value).ToList(), surgery.staffAnesthesyPhase.Select(s => s.Value).ToList());

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
                appointmentDtos.Add(AppointmentMapper.ToDtoUI(app, priority, patientMedicalRecordNumber, opRoomNumber, [], []));
            }
            return appointmentDtos;
        }

        public async Task<AppointmentDto> AddWithMedicalTeamAsync(CreateAppointmentWithMedicalTeam appointmentDto)
        {
            var opRoom = await checkOperationRoomByRodrigo(appointmentDto);
            var opRequest = await CheckOperationRequestAsync(new OperationRequestId(appointmentDto.OperationRequestId));

            var requestedDate = DateOnly.Parse(appointmentDto.AppointmentTimeSlotDtoDate); //"yyyy-MM-dd"
            var requestedStart = int.Parse(appointmentDto.AppointmentTimeSlotDtoTimeSlotStartMinute);

            // Verifica se a sala está disponível usando o método do domínio
            var opType = await _operationTypeRepository.GetByIdAsync(new OperationTypeId(opRequest.operationTypeId));

            int duracaoAnestesia = opType.preparationPhase.duration;
            int duracaoCirurgia = opType.surgeryPhase.duration;

            int endMinute = requestedStart + duracaoAnestesia + duracaoCirurgia;

            if (!opRoom.IsAvailable(
                requestedDate,
                requestedStart,
                endMinute))
            {
                throw new Exception("The operation room is occupied during the requested time.");
            }

            if (!opRequest.IsAvailable(opRequest.status))
            {
                throw new Exception("The operation request is cancelled or was already accepted.");
            }

            foreach (StaffId staff in appointmentDto.StaffAnesthesyPhase.Select(id => new StaffId(id)))
            {
                var availabilitySlot = await _availabilitySlotsRepository.GetByStaffIdAsync(staff.Value);
                if (!availabilitySlot.IsAvailable(requestedDate, requestedStart, endMinute))
                {
                    throw new Exception("The staff member is not available during the requested time.");
                }
            }

            foreach (StaffId staff in appointmentDto.StaffSurgeryPhase.Select(id => new StaffId(id)))
            {
                var availabilitySlot = await _availabilitySlotsRepository.GetByStaffIdAsync(staff.Value);
                if (!availabilitySlot.IsAvailable(requestedDate, requestedStart, endMinute))
                {
                    throw new Exception("The staff member is not available during the requested time.");
                }
            }
            //FALTA ADICIONAR O STAFF AOS RESPECTIVOS PHASES
            Console.WriteLine("Passou a verificação de disponibilidade");
            // Cria o agendamento
            var appointment = new Appointment(
                new AppointmentTimeSlot(
                    requestedDate,
                    new TimeSlot(requestedStart, endMinute)),
                new OperationRoomId(appointmentDto.OperationRoomId),
                new OperationRequestId(appointmentDto.OperationRequestId)
            );
            Console.WriteLine("Passou a criar o agendamento");
            opRoom.Appointments.Add(appointment);
            opRequest.Accepted();

            Console.WriteLine("Passou a adicionar o agendamento à sala e a aceitar o pedido de operação");

            // Atualiza o estado da sala no repositório
            // Salva o agendamento no repositório
            await _appointmentRepository.AddAsync(appointment);

            Console.WriteLine("Passou a adicionar o agendamento ao repositório");
            // Salva todas as mudanças no banco de dados
            await _unitOfWork.CommitAsync();

            return AppointmentMapper.ToDto(appointment);

        }






        public async Task<List<AppointmentDtoInTable>> GetByMedicalRecordNumberAsync(string medicalRecordNumber)
        {
            // Get all appointments
            List<Appointment> appointments = await this._appointmentRepository.GetAllAsync();
            List<AppointmentDtoInTable> appointmentDtos = new List<AppointmentDtoInTable>();
            var patient = await _patientRepository.GetByMedicalRecordNumberAsync(medicalRecordNumber);

            if (patient == null)
            {
                throw new BusinessRuleValidationException("Patient not found");
            }

            var patientId = patient.Id;

            foreach (var app in appointments)
            {
                DateOnly? appointmentDate = app?.AppointmentTimeSlot?.Date;
                int? appointmentTimeSlotStart = app?.AppointmentTimeSlot?.TimeSlot.StartMinute;
                int? appointmentTimeSlotEnd = app?.AppointmentTimeSlot?.TimeSlot.EndMinute;

                var opRequest = await _operationRequestRepository.GetByIdAsync(app.OperationRequestId);

                if (opRequest != null)
                {
                    if (opRequest.patientId == patientId.AsString())
                    {
                        string opRequestPriotity = opRequest.priority?.priority;
                        string opRequestDoctorId = opRequest.doctorThatWillPerformId;

                        var doctor = await _staffRepository.GetByIdAsync(new StaffId(opRequestDoctorId));
                        string doctorName = doctor?.FullName?.fullName;

                        var opRoom = await _operationRoomRepository.GetByIdAsync(app.OperationRoomId);
                        string opRoomNumber = opRoom?.RoomNumber?.roomNumber;

                        if (opRequestPriotity != null && opRoomNumber != null && appointmentDate != null && appointmentTimeSlotStart != null && appointmentTimeSlotEnd != null)
                        {
                            appointmentDtos.Add(new AppointmentDtoInTable(opRequestPriotity, doctorName, appointmentDate, appointmentTimeSlotStart, appointmentTimeSlotEnd, opRoomNumber));
                        }
                    }
                }
            }

            return appointmentDtos;
        }



        public async Task<StaffForSurgeryDto> GetStaffAvailableForDoinSurgeryAtCertainTime(string startMinute, string date, string operationRequestId)
        {
            Console.WriteLine("Entrou no metodo");
            var opRequest = await _operationRequestRepository.GetByIdAsync(new OperationRequestId(operationRequestId));
            Console.WriteLine("Passou apanhar o opRequest");
            var opType = await _operationTypeRepository.GetByIdAsync(new OperationTypeId(opRequest.operationTypeId));
            Console.WriteLine("Passou apanhar o opType");
            var listOfstaffNeededForAnesthesyPhase = opType.preparationPhase.requiredStaff;
            Console.WriteLine("Passou apanhar o listOfstaffNeededForAnesthesyPhase");
            var listOfstaffNeededForSurgeryPhase = opType.surgeryPhase.requiredStaff;
            Console.WriteLine("Passou apanhar o listOfstaffNeededForSurgeryPhase");
            //Pode ser melhorado, ir buscar todos nao era necessario
            List<Staff> staffAvailable = await _staffRepository.GetAllAsync();
            Console.WriteLine("Passou apanhar o staffAvailable com tamanho: " + staffAvailable.Count);
            int duracaoAnestesia = opType.preparationPhase.duration;
            int duracaoCirurgia = opType.surgeryPhase.duration;
            Console.WriteLine("Duracao fase de anestesia" + duracaoAnestesia);
            int endMinute = int.Parse(startMinute) + duracaoAnestesia + duracaoCirurgia;
            int startMinuteSurgery = int.Parse(startMinute) + duracaoAnestesia;
            List<SpecializationAndStaffDto> staffAnesthesyPhaseToBeShowed = await percorreListDeStaffsParaCadaPhaseAsync(staffAvailable, listOfstaffNeededForAnesthesyPhase, date, int.Parse(startMinute), endMinute);
            Console.WriteLine("Passou apanhar o staffAnesthesyPhaseToBeShowed");
            List<SpecializationAndStaffDto> staffSurgeryPhaseToBeShowed = await percorreListDeStaffsParaCadaPhaseAsync(staffAvailable, listOfstaffNeededForSurgeryPhase, date, startMinuteSurgery, endMinute);
            Console.WriteLine("Passou apanhar o staffSurgeryPhaseToBeShowed");
            StaffForSurgeryDto staffForSurgeryDto = new StaffForSurgeryDto(staffAnesthesyPhaseToBeShowed, staffSurgeryPhaseToBeShowed);

            return staffForSurgeryDto;
        }


        private async Task<List<SpecializationAndStaffDto>> percorreListDeStaffsParaCadaPhaseAsync(List<Staff> ALlTheStaff, List<RequiredStaff> StaffPercorrer, String date, int startMinute, int endMinute)
        {
            List<SpecializationAndStaffDto> staffForPhaseToBeShowed = new List<SpecializationAndStaffDto>();
            Console.WriteLine();
            Console.WriteLine();
            Console.WriteLine("Entrou no metodo percorreListDeStaffsParaCadaPhaseAsync");

            foreach (var staff in StaffPercorrer)
            {
                var numberStaffWithCertainSpecialization = staff.num;
                Console.WriteLine("Entrou no foreach");
                Console.WriteLine("Number necessario-->" + numberStaffWithCertainSpecialization);
                Console.WriteLine("Id Specialization necessaria-->" + staff.specialization.AsString());
                var staffAvailableForSurgery = ALlTheStaff.FindAll(x => x.SpecializationId == staff.specialization);
                Console.WriteLine("Staff existente da especialização-->" + staffAvailableForSurgery.Count);
                validateIfNumberOfElementsInListIfTheRequestIfNotThrowException(staffAvailableForSurgery, numberStaffWithCertainSpecialization);

                List<String> IdsStaffDisponivel = percorreListDeStaffsAndVerificaDisponibilidadeAsync(staffAvailableForSurgery, date, startMinute, endMinute).Result;
                Console.WriteLine("Staff needed-->" + numberStaffWithCertainSpecialization);
                Console.WriteLine("Numero de staff q esta disponivel" + IdsStaffDisponivel.Count);
                validateIfNumberOfElementsInListIfTheRequestIfNotThrowException(IdsStaffDisponivel, numberStaffWithCertainSpecialization);
                SpecializationAndStaffDto staffOfOneSpecializationSurgeryPhase = new SpecializationAndStaffDto(staff.specialization.AsString(), IdsStaffDisponivel, numberStaffWithCertainSpecialization.ToString());
                staffForPhaseToBeShowed.Add(staffOfOneSpecializationSurgeryPhase);
                Console.WriteLine();
                Console.WriteLine();
            }

            return staffForPhaseToBeShowed;
        }





        private async Task<List<string>> percorreListDeStaffsAndVerificaDisponibilidadeAsync(List<Staff> StaffPercorrer, String date, int startMinute, int endMinute)
        {
            Console.WriteLine();
            Console.WriteLine();
            Console.WriteLine("Entrou no metodo percorreListDeStaffsAndVerificaDisponibilidadeAsync");
            List<String> staffIdsList = new List<String>();
            foreach (var staff in StaffPercorrer)
            {
                Console.WriteLine("Entrou no foreach");
                var availabilitySlot = await _availabilitySlotsRepository.GetByStaffIdAsync(staff.Id.AsString());
                Console.WriteLine("Passou apanhar o availabilitySlot do staff" + staff.Id.AsString());
                Console.WriteLine("AvailabilitySlot-->" + availabilitySlot.Id.AsString() + "StaffId-->" + staff.Id.AsString());
                Boolean result = availabilitySlot.IsAvailable(DateOnly.Parse(date), startMinute, endMinute);
                Console.WriteLine("Resultado da disponibilidade para o staff-->" + staff.Id.AsString() + "-->" + result);
                if (result == true)
                {
                    staffIdsList.Add(staff.Id.AsString());
                }
            }
            Console.WriteLine("Numero de staff q esta disponivel" + staffIdsList.Count);
            Console.WriteLine();
            Console.WriteLine();
            return staffIdsList;
        }


        private void validateIfNumberOfElementsInListIfTheRequestIfNotThrowException<T>(List<T> list, int numberNeeded)
        {
            if (list.Count < numberNeeded)
            {
                throw new BusinessRuleValidationException("There are not enough staff members available for the anesthesy phase");
            }
        }

    }
}










