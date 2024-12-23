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


        public static EditingAppointmentDto ToEditingDto(Appointment appointment, List<string> anesthesiaStaff, List<string> surgeryStaff)
        {
            EditAppointmentTimeSlotDto appointmentTimeSlot = new EditAppointmentTimeSlotDto
            {
                date = appointment.AppointmentTimeSlot.Date.ToString("yyyy-MM-dd"),
                timeSlot = new TimeSlotStringDto
                {
                    StartTime = appointment.AppointmentTimeSlot.TimeSlot.StartMinute.ToString(),
                    EndTime = appointment.AppointmentTimeSlot.TimeSlot.EndMinute.ToString()
                }
            };

            return new EditingAppointmentDto
            {
            Id = appointment.Id.AsGuid(),
            anesthesiaStaff = anesthesiaStaff,
            surgeryStaff = surgeryStaff,
            OperationRoomId = appointment.OperationRoomId.Value,
            appointmentTimeSlot = appointmentTimeSlot,
            AppointmentStatus = appointment.AppointmentStatus.ToString()
            };
        }

        public static AppointmentDtoUI ToDtoUI(Appointment appointment, string operationRequestPriority, string operationRequestPatientId, string operationRoomNumber,  List<string> surgeryStaff, List<string> anesthesiaStaff)
        {
            AppointmentTimeSlotDto appointmentTimeSlot = AppointmentTimeSlotMapper.ToDto(appointment.AppointmentTimeSlot);
            // Mapear as propriedades de Appointment para AppointmentDto

            return new AppointmentDtoUI(
                appointment.Id.AsGuid(),
                operationRequestPriority,
                operationRequestPatientId,
                appointmentTimeSlot,
                appointment.AppointmentStatus.ToString(),
                operationRoomNumber,
                surgeryStaff,
                anesthesiaStaff
            );
        }
    }
}
