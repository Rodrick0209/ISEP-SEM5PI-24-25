using System;

namespace DDDSample1.Domain.AvailabilitySlots
{
    public class AvailabilitySlotMapper
    {
        public static AvailabilitySlotDto ToDTO(AvailabilitySlot obj)
        {
            return new AvailabilitySlotDto(
                obj.Id.AsGuid(),
                obj.Date.dateOfSlot.ToString("yyyy-MM-dd"), // Formata a data para string no formato esperado pelo construtor
                obj.StartTime.ToString(@"hh\:mm"), // Converte o TimeSpan para string
                obj.EndTime.ToString(@"hh\:mm"),   // Converte o TimeSpan para string
                obj.StaffMemberId
            );
        }


    }
}
