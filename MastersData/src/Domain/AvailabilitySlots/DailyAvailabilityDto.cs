using System;
using System.Collections.Generic;

namespace DDDSample1.Domain.AvailabilitySlots
{
    public class DailyAvailabilityDto
    {
        public DateOnly Date { get; set; }
        public List<TimeSlotDto> TimeSlots { get; set; }

        public DailyAvailabilityDto(DateOnly date, List<TimeSlotDto> timeSlots)
        {
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