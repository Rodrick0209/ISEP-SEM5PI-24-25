using System;
using System.Collections.Generic;
using System.Linq;
using DDDSample1.Domain.Utils;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.Appointments;
using Org.BouncyCastle.Asn1.Cms;

namespace DDDSample1.Domain.Appointments
{
    public class AppointmentTimeSlotDto
    {
        public DateOnly Date { get; private set; }
        public TimeSlotDto TimeSlot { get; private set; }


        public AppointmentTimeSlotDto(DateOnly date, TimeSlotDto timeSlot)
        {


            Date = date;
            TimeSlot = timeSlot;


        }







    }
}
