using DDDSample1.Domain.Appointments;
using System;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.Utils;


public static class AppointmentTimeSlotMapper
{
    public static AppointmentTimeSlotDto ToDto(AppointmentTimeSlot appointmentTimeSlot)
    {
        return new AppointmentTimeSlotDto(appointmentTimeSlot.Date, new TimeSlotDto(appointmentTimeSlot.TimeSlot.StartMinute, appointmentTimeSlot.TimeSlot.EndMinute));
    }
}

