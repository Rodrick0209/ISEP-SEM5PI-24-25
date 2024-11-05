using System;
using System.Collections.Generic;

namespace DDDSample1.Domain.AvailabilitySlots
{
    public class AvailabilitySlotDto
    {
        public string Id { get; set; }
        public string StaffId { get; set; }
        public List<DailyAvailabilityDto> DailyAvailabilities { get; set; }

        public AvailabilitySlotDto(string id, string staffId  ,List<DailyAvailabilityDto> dailyAvailabilities)
        {
            Id = id;
            StaffId = staffId;
            DailyAvailabilities = dailyAvailabilities;
        }





    }
}
