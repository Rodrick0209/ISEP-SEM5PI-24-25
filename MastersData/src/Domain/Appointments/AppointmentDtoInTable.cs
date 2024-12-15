using System;
using DDDSample1.Domain.Utils;

namespace DDDSample1.Domain.Appointments
{
    public class AppointmentDtoInTable
    {
        public string Priority { get; set; }
        public string Doctor { get; set; }
        public AppointmentTimeSlotDto AppointmentTimeSlot { get; set; }
        public string RoomNumber { get; set; }

        public AppointmentDtoInTable(string priority, string doctor, DateOnly? date, int? startTime, int? endTime, string roomNumber)
        {
            Priority = priority;
            Doctor = doctor;
            var slot = new TimeSlotDto(startTime ?? default(int), endTime ?? default(int));
            AppointmentTimeSlot = new AppointmentTimeSlotDto(date ?? default(DateOnly), slot);
            RoomNumber = roomNumber;
        }
    }
}