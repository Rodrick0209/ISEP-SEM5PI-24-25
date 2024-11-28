using System;
using System.Collections.Generic;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.OperationRooms;
using DDDSample1.Domain.Utils; // Esta linha deve estar presente
using System.Linq;
using DDDSample1.Domain.Appointments; // Ajuste conforme necessário para o namespace correto

namespace DDDSample1.Domain.OperationRooms
{
    public class OperationRoom : Entity<OperationRoomId>, IAggregateRoot
    {
        public RoomNumber RoomNumber { get; private set; } // Identificador da sala
        public RoomType RoomType { get; private set; } // Tipo da sala
        public RoomCapacity RoomCapacity { get; private set; } // Capacidade da sala
        public RoomStatus RoomStatus { get; private set; } // Status atual da sala
        public List<MaintenanceSlots> MaintenanceSlots { get; private set; } // Agendas de manutenção

        public List<Appointment> Appointments { get; private set; } // Agendamentos

        private OperationRoom() { }
        public OperationRoom(string roomNumber, string roomType, string roomCapacity, List<MaintenanceSlots> maintenanceSlots, List<Appointment> appointments)
        {
            this.Id = new OperationRoomId(Guid.NewGuid());
            this.RoomNumber = new RoomNumber(roomNumber);
            this.RoomType = new RoomType(roomType);
            this.RoomCapacity = new RoomCapacity(roomCapacity);
            this.RoomStatus = RoomStatus.Available; // Status padrão
            this.MaintenanceSlots = maintenanceSlots;
            this.Appointments = appointments;
        }

        public OperationRoom(string roomNumber, string roomType, string roomCapacity)
        {
            this.Id = new OperationRoomId(Guid.NewGuid());
            this.RoomNumber = new RoomNumber(roomNumber);
            this.RoomType = new RoomType(roomType);
            this.RoomCapacity = new RoomCapacity(roomCapacity);
            this.RoomStatus = RoomStatus.Available; // Status padrão
            this.MaintenanceSlots = new List<MaintenanceSlots>();
            this.Appointments = new List<Appointment>();
        }

        public void ChangeStatus(RoomStatus newStatus)
        {
            RoomStatus = newStatus;
        }


        public void EnsureWithinCapacity(int numberOfPeople)
        {
            RoomCapacity.EnsureWithinCapacity(numberOfPeople);
        }

        public void AddMaintenance(DateOnly date, int startMinute, int endMinute)
        {
            TimeSlot timeSlot = new TimeSlot(startMinute, endMinute);

            var maintenanceAvailability = this.MaintenanceSlots.FirstOrDefault(maint => maint.Date == date);
            if (maintenanceAvailability == null)
            {
                maintenanceAvailability = new MaintenanceSlots(date);
                maintenanceAvailability.AddTimeSlot(startMinute, endMinute);
                this.MaintenanceSlots.Add(maintenanceAvailability);
            }
            else
            {
                maintenanceAvailability.AddTimeSlot(startMinute, endMinute);
            }
        }



        public bool IsAvailable(DateTime currentTime)
        {
            var currentDate = DateOnly.FromDateTime(currentTime);
            var currentMinute = currentTime.Hour * 60 + currentTime.Minute;

            // Verifica se existe uma manutenção no horário atual
            var maintenanceToday = this.MaintenanceSlots.FirstOrDefault(slot => slot.Date == currentDate);

            if (maintenanceToday != null && maintenanceToday.TimeSlots.Any(ts => ts.StartMinute <= currentMinute && ts.EndMinute >= currentMinute))
            {
                ChangeStatus(RoomStatus.UnderMaintenance);
                return false;
            }

            // Atualiza status para disponível, se não houver manutenção
            ChangeStatus(RoomStatus.Available);
            return true;
        }


    }
}
