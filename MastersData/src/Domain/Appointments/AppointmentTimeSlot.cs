using System;
using System.Collections.Generic;
using System.Linq;
using DDDSample1.Domain.Utils;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.Appointments;
using Org.BouncyCastle.Asn1.Cms;

namespace DDDSample1.Domain.Appointments
{
    public class AppointmentTimeSlot : IValueObject
    {
        public DateOnly Date { get; private set; }
        public TimeSlot TimeSlot { get; private set; }


        public AppointmentTimeSlot()
        {

        }


        public AppointmentTimeSlot(DateOnly date, TimeSlot timeSlot)
        {
            if (timeSlot == null)
            {
                throw new ArgumentException("O TimeSlot não pode ser nulo.");
            }

            Date = date;
            TimeSlot = timeSlot;




            if (IsInThePast())
            {
                throw new InvalidOperationException("O Appointment não pode estar no passado.");
            }
        }

        // Método para verificar se a data e hora estão no passado
        private bool IsInThePast()
        {
            // Obtém a data e hora atuais
            var currentDateTime = DateTime.Now;

            // Cria um DateTime a partir da DateOnly e TimeSlot fornecidos
            var appointmentDateTime = Date.ToDateTime(new TimeOnly(TimeSlot.StartMinute / 60, TimeSlot.StartMinute % 60));

            // Compara a data e hora
            return appointmentDateTime < currentDateTime;
        }

        public bool IsTodayAndWithinTimeSlot()
        {
            var currentDate = DateOnly.FromDateTime(DateTime.Now);
            var currentTime = TimeOnly.FromDateTime(DateTime.Now);
            return Date == currentDate && TimeSlot.IsBetween(currentTime);
        }
    
        public bool IsHappeningAt(DateOnly date, TimeOnly time)
        {
            return Date == date && TimeSlot.IsBetween(time);
        }
    }







}

