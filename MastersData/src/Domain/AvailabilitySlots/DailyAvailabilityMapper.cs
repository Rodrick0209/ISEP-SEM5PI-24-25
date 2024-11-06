using System;
using System.Collections.Generic;
using DDDSample1.Domain.Utils;


namespace DDDSample1.Domain.AvailabilitySlots
{


    public static class DailyAvailabilityMapper
    {
        public static DailyAvailabilityDto ToDto(DailyAvailability dailyAvailability)
        {
            Console.WriteLine("Erro aqui DAILY AVAILABLE MAPPER");
            return new DailyAvailabilityDto ( dailyAvailability.Date, dailyAvailability.TimeSlots.ConvertAll(slot => new TimeSlotDto(slot.StartMinute, slot.EndMinute)));
        }

        
    }
}