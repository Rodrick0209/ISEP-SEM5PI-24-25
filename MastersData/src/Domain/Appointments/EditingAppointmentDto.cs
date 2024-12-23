using System;
using System.Collections.Generic;
using DDDSample1.Domain.OperationRooms;
using DDDSample1.Domain.OperationRequest;
using DDDSample1.Domain.Utils;

namespace DDDSample1.Domain.Appointments
{
    public class EditingAppointmentDto
    {
        public Guid Id { get; set; }
        public List<string>? anesthesiaStaff { get; set; }
        public List<string>? surgeryStaff { get; set; }
        public string? OperationRoomId { get; set; }
        public EditAppointmentTimeSlotDto appointmentTimeSlot { get; set; }

        public string? AppointmentStatus { get; set; }

    }
    public class EditAppointmentTimeSlotDto
    {
        public string date { get; set; }
        public TimeSlotStringDto timeSlot { get; set; }
    }
    public class TimeSlotStringDto
    {
        public string StartTime { get; set; }
        public string EndTime { get; set; }
    }
    


}