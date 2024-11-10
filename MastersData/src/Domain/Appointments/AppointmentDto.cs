using System;
using System.Collections.Generic;
using DDDSample1.Domain.OperationRooms;
using DDDSample1.Domain.OperationRequest;

namespace DDDSample1.Domain.Appointments
{
    public class AppointmentDto
    {
        public Guid AppointmentId { get; set; }
        public AppointmentTimeSlotDto AppointmentTimeSlotDto { get; set; }
        public string AppointmentStatus { get; set; }
        public string OperationRoomId { get; set; }
        public string OperationRequestId { get; set; }

        public AppointmentDto(Guid appointmentId, AppointmentTimeSlotDto appointmentTimeSlot, string appointmentStatus, string operationRoomId, string operationRequestId)
        {
            AppointmentId = appointmentId;
            AppointmentTimeSlotDto = appointmentTimeSlot;
            AppointmentStatus = appointmentStatus;
            OperationRoomId = operationRoomId;
            OperationRequestId = operationRequestId;
        }

        public AppointmentDto()
        {
        }
    }
}
