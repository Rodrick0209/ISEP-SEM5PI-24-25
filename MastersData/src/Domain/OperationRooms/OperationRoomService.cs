using System.Collections.Generic;
using System;
using System.Threading.Tasks;
using DDDSample1.Domain.OperationRooms;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.Appointments;

namespace DDDSample1.Domain.OperationRooms
{
    public class OperationRoomService : IOperationRoomService
    {
        private readonly IUnitOfWork _unitOfWork;
        private readonly IOperationRoomRepository _operationRoomRepository;

        private readonly IAppointmentRepository _appointmentRepository;

        public OperationRoomService(IUnitOfWork unitOfWork, IOperationRoomRepository operationRoomRepository,IAppointmentRepository appointmentRepository)
        {
            this._unitOfWork = unitOfWork;
            this._operationRoomRepository = operationRoomRepository;
            this._appointmentRepository= appointmentRepository;
        }

        public async Task<List<OperationRoom>> GetAllAsync()
        {
            return await this._operationRoomRepository.GetAllAsync();
        }

        public async Task<List<OperationRoom>> GetOccupiedAsync(DateOnly date, TimeOnly time)
        {
            List<Appointment> appointments = await _appointmentRepository.GetAppointmentsByDateTimeAsync(date,time);

            List<OperationRoom> occupiedRooms = new List<OperationRoom>();

            foreach (var appointment in appointments)
            {
                var room = await _operationRoomRepository.GetByIdAsync(appointment.OperationRoomId);
                if (room != null)
                {
                    occupiedRooms.Add(room);
                }
            }

            return occupiedRooms;
        }


    }
}
