using System;
using DDDSample1.Domain.Shared;



namespace DDDSample1.Domain.Utils
{
    public class TimeSlot : IValueObject
    {
        public int StartMinute { get; private set; }
        public int EndMinute { get; private set; }

        public TimeSlot(int startMinute, int endMinute)
        {
            if (startMinute < 0 || endMinute > 1440 || startMinute >= endMinute)
                throw new ArgumentException("Intervalo de tempo inválido");

            StartMinute = startMinute;
            EndMinute = endMinute;
        }
    }

}