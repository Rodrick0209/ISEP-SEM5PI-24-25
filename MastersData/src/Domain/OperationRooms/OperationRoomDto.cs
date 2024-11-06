using System;
using System.Collections.Generic;
using DDDSample1.Domain.OperationRooms;
using DDDSample1.Domain.Shared;




namespace DDDSample1.Domain.OperationRooms
{
    public class OperationRoomDto
    {
        public string Id { get; set; }
        public string RoomNumber { get; set; }
        public string RoomType { get; set; }
        public string RoomCapacity { get; set; }
        public string RoomStatus { get; set; }
        public List<MaintenanceSlotsDto> MaintenanceSlots { get; set; }

        public OperationRoomDto(string id, string roomNumber, string roomType, string roomCapacity, string roomStatus, List<MaintenanceSlotsDto> maintenanceSlots)
        {
            Id = id;
            RoomNumber = roomNumber;
            RoomType = roomType;
            RoomCapacity = roomCapacity;
            RoomStatus = roomStatus;
            MaintenanceSlots = maintenanceSlots;
        }
    }
}
