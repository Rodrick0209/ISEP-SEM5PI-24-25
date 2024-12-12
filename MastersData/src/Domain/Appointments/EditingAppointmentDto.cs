using System;
using System.Collections.Generic;
using DDDSample1.Domain.OperationRooms;
using DDDSample1.Domain.OperationRequest;

namespace DDDSample1.Domain.Appointments
{
    public class EditingAppointmentDto
    {
        public Guid Id { get; set; }
        public List<string>? OperationRequestTeamForAnesthesy { get; set; }
        public List<string>? OperationRequestTeamForSurgery { get; set; }
        public string? OperationRoomId { get; set; }
        public string AppointmentTimeSlotDtoDate { get; set; }
        public string AppointmentTimeSlotDtoTimeSlotStartMinute { get; set; }
        public string AppointmentTimeSlotDtoTimeSlotEndMinute { get; set; }
        public string? AppointmentStatus { get; set; }

    }
}