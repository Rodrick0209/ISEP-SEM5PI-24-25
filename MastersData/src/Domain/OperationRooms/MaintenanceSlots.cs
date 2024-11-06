using System;
using System.Collections.Generic;
using System.Linq;
using DDDSample1.Domain.Utils;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.OperationRooms;

namespace DDDSample1.Domain.OperationRooms
{
    public class MaintenanceSlots : IValueObject
    {
        public DateOnly Date { get; private set; }
        public List<TimeSlot> TimeSlots { get; private set; }


        public MaintenanceSlots(DateOnly date)
        {


            Date = date;
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
