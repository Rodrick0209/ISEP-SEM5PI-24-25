using System;
using System.Collections.Generic;
using DDDSample1.Domain.OperationRooms;

namespace DDDSample1.Domain.Appointments
{
    public class AppointmentMapper
    {
        public static AppointmentDto ToDTO(Appointment appointment,  Dictionary<OperationRoomId, RoomNumber> roomMap)
        {

            AppointmentTimeSlotDto appointmentTimeSlot = AppointmentTimeSlotMapper.ToDto(appointment.AppointmentTimeSlot);
            // Mapear as propriedades de Appointment para AppointmentDto

            return new AppointmentDto(
                appointment.Id.AsGuid(), 
                appointmentTimeSlot,
                appointment.AppointmentStatus.ToString(), 
                roomMap[appointment.OperationRoomId].roomNumber, 
                appointment.OperationRequestId.Value
            );
        }
        
        public static AppointmentDto ToDto(Appointment appointment)
        {

            AppointmentTimeSlotDto appointmentTimeSlot = AppointmentTimeSlotMapper.ToDto(appointment.AppointmentTimeSlot);
            // Mapear as propriedades de Appointment para AppointmentDto

            return new AppointmentDto(
                appointment.Id.AsGuid(), 
                appointmentTimeSlot,
                appointment.AppointmentStatus.ToString(), 
                appointment.OperationRoomId.Value,
                appointment.OperationRequestId.Value
            );
        }

        public static AppointmentDtoUI ToDtoUI(Appointment appointment, string operationRequestPriority, string operationRequestPatientId, string operationRoomNumber)
        {
            AppointmentTimeSlotDto appointmentTimeSlot = AppointmentTimeSlotMapper.ToDto(appointment.AppointmentTimeSlot);
            // Mapear as propriedades de Appointment para AppointmentDto

            return new AppointmentDtoUI(
                operationRequestPriority,
                operationRequestPatientId,
                appointmentTimeSlot,
                appointment.AppointmentStatus.ToString(),
                operationRoomNumber
            );
        }
    }
}
