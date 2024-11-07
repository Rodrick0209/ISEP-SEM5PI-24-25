using System;
using System.Collections.Generic;

namespace DDDSample1.Domain.Appointments
{
    public class AppointmentMapper
    {
        public static AppointmentDto ToDTO(Appointment appointment)
        {

            AppointmentTimeSlotDto appointmentTimeSlot = AppointmentTimeSlotMapper.ToDto(appointment.AppointmentTimeSlot);
            // Mapear as propriedades de Appointment para AppointmentDto
            return new AppointmentDto(
                appointment.Id.AsGuid(), 
                appointmentTimeSlot,
                appointment.AppointmentStatus.ToString(), 
                appointment.OperationRoomId, 
                appointment.OperationRequestId
            );
        }
    }
}
