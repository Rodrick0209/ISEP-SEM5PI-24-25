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
            // Validação explícita para o início do intervalo
            if (startMinute >= 1440)
                throw new ArgumentException("O horário de início ultrapassa o final do dia");

            // Validação geral
            if (startMinute < 0 || endMinute > 1440 || startMinute >= endMinute)
                throw new ArgumentException("Intervalo de tempo inválido");

            StartMinute = startMinute;
            EndMinute = endMinute;
        }



        public bool IsBetween(TimeOnly hour)
        {
            int minuteInMinutes = hour.Hour * 60 + hour.Minute;
            return minuteInMinutes >= StartMinute && minuteInMinutes <= EndMinute;
        }
    }

}