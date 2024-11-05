using System;
using System.Collections.Generic;

namespace DDDSample1.Domain.AvailabilitySlots
{
    public class DailyAvailabilityDto
    {
        public Guid Id { get; set; }
        public DateOnly Date { get; set; }
        public List<TimeSlotDto> TimeSlots { get; set; }

        public DailyAvailabilityDto(Guid id, DateOnly date, List<TimeSlotDto> timeSlots)
        {
            Id = id;
            Date = date;
            TimeSlots = timeSlots;
        }
    }

    public class TimeSlotDto
    {
        public int StartMinute { get; set; }
        public int EndMinute { get; set; }

        public TimeSlotDto(int startMinute, int endMinute)
        {
            StartMinute = startMinute;
            EndMinute = endMinute;
        }
    }
}