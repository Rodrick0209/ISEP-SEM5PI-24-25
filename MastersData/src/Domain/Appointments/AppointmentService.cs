using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using DDDSample1.Domain.Appointments;
using DDDSample1.Domain.OperationRequest;
using DDDSample1.Domain.OperationRooms;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.Utils;

namespace DDDSample1.Domain.Appointments
{
    public class AppointmentService : IAppointmentService
    {
        private readonly IUnitOfWork _unitOfWork;
        private readonly IAppointmentRepository _appointmentRepository;

        private readonly IOperationRoomRepository _operationRoomRepository;
        private readonly IOperationRequestRepository _operationRequestRepository;

        public AppointmentService(IUnitOfWork unitOfWork, IAppointmentRepository appointmentRepository, IOperationRoomRepository operationRoomRepository, IOperationRequestRepository operationRequestRepository)
        {
            this._unitOfWork = unitOfWork;
            this._appointmentRepository = appointmentRepository;
            this._operationRoomRepository = operationRoomRepository;
            this._operationRequestRepository = operationRequestRepository;
        }


        public async Task<AppointmentDto> AddAsync(AppointmentDto appointmentDto)
        {


            await checkOperationRoomByNameAsync(appointmentDto.OperationRoomId, appointmentDto);
            await CheckOperationRequestAsync(new OperationRequestId(appointmentDto.OperationRequestId));


            var appointment = new Appointment(new AppointmentTimeSlot(appointmentDto.AppointmentTimeSlotDto.Date, new TimeSlot(appointmentDto.AppointmentTimeSlotDto.TimeSlot.StartTime, appointmentDto.AppointmentTimeSlotDto.TimeSlot.EndTime)), new OperationRoomId(appointmentDto.OperationRoomId), new OperationRequestId(appointmentDto.OperationRequestId));

            await _appointmentRepository.AddAsync(appointment);
            await _unitOfWork.CommitAsync();


            return AppointmentMapper.ToDto(appointment);


        }

        public async Task<List<Appointment>> GetAllAsync()
        {
            return await this._appointmentRepository.GetAllAsync();
        }


        public async Task<OperationRoom> checkOperationRoomByNameAsync(string operationRoom, AppointmentDto appointment)
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



        public async Task<AppointmentDto> GetByIdAsync(AppointmentId id)
        {

            var app = await _appointmentRepository.GetByIdAsync(id);

            return app == null ? null : AppointmentMapper.ToDto(app);

        }

    }
}


