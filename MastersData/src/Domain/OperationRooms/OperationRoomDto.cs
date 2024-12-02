using System;
using System.Collections.Generic;
using DDDSample1.Domain.Appointments;
using DDDSample1.Domain.OperationRooms;
using DDDSample1.Domain.RoomTypes;
using DDDSample1.Domain.Shared;




namespace DDDSample1.Domain.OperationRooms
{
    public class OperationRoomDto
    {
        public Guid Id { get; set; }
        public string RoomNumber { get; set; }
        public RoomTypeDto RoomType { get; set; }
        public string RoomCapacity { get; set; }
        public string RoomStatus { get; set; }
        public List<MaintenanceSlotsDto> MaintenanceSlots { get; set; }
        public List<AppointmentDto> Appointments { get; set; }

        public OperationRoomDto(Guid id, string roomNumber, RoomTypeDto roomType, string roomCapacity, string roomStatus, List<MaintenanceSlotsDto> maintenanceSlots, List<AppointmentDto> appointments)
        {
            Id = id;
            RoomNumber = roomNumber;
            RoomType = roomType;
            RoomCapacity = roomCapacity;
            RoomStatus = roomStatus;
            MaintenanceSlots = maintenanceSlots;
            Appointments = appointments;
        }

        public OperationRoomDto()
        {
        }
    }
}
