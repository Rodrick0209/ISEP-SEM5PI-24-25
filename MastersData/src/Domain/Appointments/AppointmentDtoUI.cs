using System;
using System.Collections.Generic;
using DDDSample1.Domain.OperationRooms;
using DDDSample1.Domain.OperationRequest;

namespace DDDSample1.Domain.Appointments
{
    public class AppointmentDtoUI
    {
        public Guid Id { get; set; }
        public string OperationRequestPriority { get; set; }
        public string OperationRequestPatientId { get; set; }
        public AppointmentTimeSlotDto AppointmentTimeSlot { get; set; }
        
        public string AppointmentStatus { get; set; }
        public string OperationRoomId { get; set; }

        public List<string> SurgeryStaff { get; set; }
        
        public List<string> AnesthesiaStaff { get; set; }


        public AppointmentDtoUI(Guid Id, string operationRequestPriority, string operationRequestPatientId, AppointmentTimeSlotDto appointmentTimeSlot, string appointmentStatus, string operationRoomId, List<string> surgeryStaff, List<string> anesthesiaStaff)
        {
            this.Id = Id;
            this.OperationRequestPriority = operationRequestPriority;
            this.OperationRequestPatientId = operationRequestPatientId;
            this.AppointmentTimeSlot = appointmentTimeSlot;
            this.AppointmentStatus = appointmentStatus;
            this.OperationRoomId = operationRoomId;
            this.SurgeryStaff = surgeryStaff;
            this.AnesthesiaStaff = anesthesiaStaff;
        }
    }
}