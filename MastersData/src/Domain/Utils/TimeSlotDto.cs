using System;
using DDDSample1.Domain.Shared;



namespace DDDSample1.Domain.Utils
{

    public class TimeSlotDto
    {
        public int StartTime { get; set; }
        public int EndTime { get; set; }

        public TimeSlotDto(int startTime, int endTime)
        {
            this.StartTime = startTime;
            this.EndTime = endTime;
        }
    }




}