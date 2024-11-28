using System;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.Utils;
using System.Collections.Generic;
using System.Linq;
using DDDSample1.Domain.OperationRooms;
using DDDSample1.Domain.OperationRequest;

namespace DDDSample1.Domain.Appointments
{
    public class Appointment : Entity<AppointmentId>, IAggregateRoot
    {
        public AppointmentTimeSlot AppointmentTimeSlot { get; private set; } // Hora do agendamento
        public AppointmentStatus AppointmentStatus { get; private set; }
        public OperationRoomId OperationRoomId { get; private set; } // Sala de operação associada ao agendamento
        public OperationRequestId OperationRequestId { get; private set; } // Pedido de operação associado ao agendamento

        public Appointment()
        {
        }

        public Appointment(AppointmentTimeSlot appointmentTimeSlot, OperationRoomId operationRoomId, OperationRequestId operationRequestId)
        {
            this.Id = new AppointmentId(Guid.NewGuid());
            this.AppointmentTimeSlot = appointmentTimeSlot;
            this.AppointmentStatus = AppointmentStatus.Scheduled; // Status padrão
            this.OperationRoomId = operationRoomId;
            this.OperationRequestId = operationRequestId;
        }

        public void ChangeStatus(AppointmentStatus newStatus)
        {
            AppointmentStatus = newStatus;
        }


        public override string ToString()
        {
            return $"Appointment ID: {Id.AsString()}, " +
                   $"Date: {AppointmentTimeSlot.Date}, " +
                   $"Time: {AppointmentTimeSlot.TimeSlot}, " +
                   $"Status: {AppointmentStatus}, " +
                   $"Operation Room ID: {OperationRoomId}, " +
                   $"Operation Request ID: {OperationRequestId}";
        }


        public override bool Equals(object obj)
        {
            if (obj == null || GetType() != obj.GetType())
                return false;

            Appointment other = (Appointment)obj;
            return Id.Equals(other.Id) &&
                   AppointmentTimeSlot.Date.Equals(other.AppointmentTimeSlot.Date) &&
                   AppointmentTimeSlot.TimeSlot.Equals(other.AppointmentTimeSlot.TimeSlot);
        }

        public override int GetHashCode()
        {
            return HashCode.Combine(Id, AppointmentTimeSlot.Date, AppointmentTimeSlot.TimeSlot);
        }

        public bool isHappening(){
            return AppointmentTimeSlot.IsTodayAndWithinTimeSlot();
        }

    }
}
