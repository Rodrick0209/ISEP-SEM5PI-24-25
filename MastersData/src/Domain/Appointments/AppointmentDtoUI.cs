using System;
using System.Collections.Generic;
using DDDSample1.Domain.OperationRooms;
using DDDSample1.Domain.OperationRequest;

namespace DDDSample1.Domain.Appointments
{
    public class AppointmentDtoUI
    {
        public Guid AppointmentId { get; set; }
        public string OperationRequestPriority { get; set; }
        public string OperationRequestPatientId { get; set; }
        public AppointmentTimeSlotDto AppointmentTimeSlot { get; set; }
        
        public string AppointmentStatus { get; set; }
        public string OperationRoomNumber { get; set; }



        public AppointmentDtoUI(Guid appointmentId, string operationRequestPriority, string operationRequestPatientId, AppointmentTimeSlotDto appointmentTimeSlot, string appointmentStatus, string operationRoomNumber)
        {
            this.AppointmentId = appointmentId;
            this.OperationRequestPriority = operationRequestPriority;
            this.OperationRequestPatientId = operationRequestPatientId;
            this.AppointmentTimeSlot = appointmentTimeSlot;
            this.AppointmentStatus = appointmentStatus;
            this.OperationRoomNumber = operationRoomNumber;
        }
        
    }
}