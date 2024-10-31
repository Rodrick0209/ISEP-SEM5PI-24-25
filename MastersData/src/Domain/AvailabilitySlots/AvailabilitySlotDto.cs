using System;

namespace DDDSample1.Domain.AvailabilitySlots
{
    public class AvailabilitySlotDto
    {
        public Guid Id { get; set; } // ID do slot de disponibilidade
        public string Date { get; set; } // Data do slot de disponibilidade
        public string StartTime { get; set; } // Horário de início do slot
        public string EndTime { get; set; } // Horário de término do slot

        public AvailabilitySlotDto(Guid id, string date, string startTime, string endTime)
        {
            this.Id = id;
            this.Date = date;
            this.StartTime = startTime;
            this.EndTime = endTime;
        }
    }
}
