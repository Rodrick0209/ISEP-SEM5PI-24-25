using System;
using System.Collections.Generic;
using DDDSample1.Domain.OperationRooms;

namespace DDDSample1.Domain.Appointments
{
    public class AppointmentMapper
    {
        public static AppointmentDto ToDTO(Appointment appointment,  Dictionary<string, RoomNumber> roomMap)
        {

            AppointmentTimeSlotDto appointmentTimeSlot = AppointmentTimeSlotMapper.ToDto(appointment.AppointmentTimeSlot);
            // Mapear as propriedades de Appointment para AppointmentDto

            return new AppointmentDto(
                appointment.Id.AsGuid(), 
                appointmentTimeSlot,
                appointment.AppointmentStatus.ToString(), 
                roomMap[appointment.OperationRoomId].roomNumber, 
                appointment.OperationRequestId
            );
        }
    }
}
