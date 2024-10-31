using System;
using System.Collections.Generic;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.AvailabilitySlots;
using DDDSample1.Domain.StaffMembers;

namespace DDDSample1.Domain.AvailabilitySlots
{
    public class AvailabilitySlot : Entity<AvailabilitySlotsId>, IAggregateRoot
    {





        public DateOfSlot Date { get; private set; } // Data do slot de disponibilidade
        public TimeSpan StartTime { get; private set; } // Horário de início do slot
        public TimeSpan EndTime { get; private set; } // Horário de término do slot



        public AvailabilitySlot(string date, string startTime, string endTime)
        {
            TimeSpan start = TimeSpan.Parse(startTime);
            TimeSpan end = TimeSpan.Parse(endTime);

            if (end <= start)
                throw new ArgumentException("End time must be after start time.");

            Id = new AvailabilitySlotsId(Guid.NewGuid());
            Date = new DateOfSlot(DateTime.Parse(date));
            StartTime = start;
            EndTime = end;
        }

        public bool IsAvailable(DateTime appointmentDate, string appointmentStartTime, string appointmentEndTime)
        {

            TimeSpan appointmentStartTimes = TimeSpan.Parse(appointmentStartTime);
            TimeSpan appointmentEndTimes = TimeSpan.Parse(appointmentEndTime);
            // Check if the slot is on the same day and the time is within the availability range
            return Date.dateOfSlot == appointmentDate.Date &&
                   appointmentStartTimes >= StartTime &&
                   appointmentEndTimes <= EndTime;
        }
    }
}
