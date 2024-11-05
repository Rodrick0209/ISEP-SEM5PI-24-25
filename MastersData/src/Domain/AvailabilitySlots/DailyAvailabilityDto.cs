using System;
using System.Collections.Generic;
using DDDSample1.Domain.Utils;


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



}