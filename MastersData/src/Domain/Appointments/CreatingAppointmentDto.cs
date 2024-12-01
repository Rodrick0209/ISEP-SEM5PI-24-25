using System;
using System.Collections.Generic;
using DDDSample1.Domain.OperationRooms;
using DDDSample1.Domain.OperationRequest;

namespace DDDSample1.Domain.Appointments
{
    public class CreatingAppointmentDto
    {
       
        public string AppointmentTimeSlotDtoDate { get; set; }
        public string AppointmentTimeSlotDtoTimeSlotStartMinute { get; set; }
        public string AppointmentTimeSlotDtoTimeSlotEndMinute { get; set; }
        public string OperationRoomId { get; set; }
        public string OperationRequestId { get; set; }

        
    }
}
