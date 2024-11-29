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

        //Devia ser appointMent id, assim provoca alto acoplamento
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



        public bool IsAvailable(DateOnly date, int startMinute, int endMinute)
        {
            // Inicializa as listas se forem nulas
            if (startMinute < 0 || endMinute > 1440)
            {
                return false;
            }



            if (Appointments == null)
            {
                Appointments = new List<Appointment>();
                if (startMinute >= 0 && endMinute <= 1440)
                    return true;

            }

            List<Appointment> appointmentsToSearch = getAppointmentForDate(date);
            Console.WriteLine("Appointments size to search: " + appointmentsToSearch.Count);

            // Verifica conflitos com agendamentos existentes
            foreach (var app in appointmentsToSearch)
            {
                var appStart = app.AppointmentTimeSlot.TimeSlot.StartMinute;
                Console.WriteLine("appStart: " + appStart);
                var appEnd = app.AppointmentTimeSlot.TimeSlot.EndMinute;
                Console.WriteLine("appEnd: " + appEnd);

                // Casos de sobreposição:
                if ((startMinute >= appStart && startMinute < appEnd) ||
                    (endMinute >= appStart && endMinute <= appEnd) ||
                    (startMinute <= appStart && endMinute >= appEnd))
                {
                    return false;
                }
            
            }


            // Nenhum conflito encontrado
            return true;
        }


        public List<Appointment> getAppointmentForDate(DateOnly date)
        {
            List<Appointment> result = new List<Appointment>();
            foreach (var app in Appointments)
            {
                if (app.AppointmentTimeSlot.Date.Equals(date))
                {
                    result.Add(app);
                }
            }

            return result;
        }

    }
}
