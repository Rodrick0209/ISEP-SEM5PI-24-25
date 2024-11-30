using System;
using System.Collections.Generic;
using DDDSample1.Domain.OperationRooms;
using DDDSample1.Domain.OperationRequest;

namespace DDDSample1.Domain.Appointments
{
    public class EditingAppointmentDto
    {
        public  Guid Id { get; set; }
        public string? OperationRequestId { get; set; }
        public string? OperationRoomId { get; set; }
        public AppointmentTimeSlotDto? AppointmentTimeSlot { get; set; }
        public string? AppointmentStatus { get; set; }
        
    }
}