using System;
using System.Collections.Generic;
using System.Linq;
using DDDSample1.Domain.Shared;


namespace DDDSample1.Domain.AvailabilitySlots
{
    public class DailyAvailability : Entity<DailyAvailabilityId>
    {
        public DateOnly  Date { get; private set; }
        public List<TimeSlot> TimeSlots { get; private set; }

        public DailyAvailability(DateOnly  date)
        {
            Id = new DailyAvailabilityId(Guid.NewGuid());
            Date = date; // Armazenar apenas a data
            TimeSlots = [];
        }

        public void AddTimeSlot(int startMinute, int endMinute)
        {
            //falta validacao aqui
            var timeSlot = new TimeSlot(startMinute, endMinute);
            TimeSlots.Add(timeSlot);
        }

        // Verifica se algum slot está ocupado em relação a um intervalo de tempo
        public bool IsOccupied(int startMinute, int endMinute)
        {
            return TimeSlots.Any(slot =>
                startMinute < slot.EndMinute && endMinute > slot.StartMinute);
        }



    }
}
