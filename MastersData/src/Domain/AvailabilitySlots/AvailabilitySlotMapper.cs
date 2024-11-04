using System;
using System.Collections.Generic;

namespace DDDSample1.Domain.AvailabilitySlots
{
    public class AvailabilitySlotMapper
    {

        public static AvailabilitySlotDto ToDTO(AvailabilitySlot availabilitySlot)
        {
            List<DailyAvailabilityDto> dailyAvailabilities = availabilitySlot.Availability.ConvertAll(DailyAvailabilityMapper.ToDto);
            return new AvailabilitySlotDto(availabilitySlot.Id.AsString(),availabilitySlot.StaffId ,dailyAvailabilities);
            
        }

    }
}
