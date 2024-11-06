using System;
using System.Collections.Generic;

namespace DDDSample1.Domain.AvailabilitySlots
{
    public class AvailabilitySlotDto
    {
        public string StaffId { get; set; }
        public List<DailyAvailabilityDto> DailyAvailabilities { get; set; }

        public AvailabilitySlotDto(string staffId  ,List<DailyAvailabilityDto> dailyAvailabilities)
        {
            StaffId = staffId;
            DailyAvailabilities = dailyAvailabilities;
        }





    }
}
