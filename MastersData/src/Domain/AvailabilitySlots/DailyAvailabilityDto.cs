using System;
using System.Collections.Generic;
using DDDSample1.Domain.Utils;


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



}