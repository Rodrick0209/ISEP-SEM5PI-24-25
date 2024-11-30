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



        public bool IsAvailable(DateOnly date, int startMinute, int endMinute)
{
    // Inicializa as listas se forem nulas
    if (Appointments == null)
        Appointments = new List<Appointment>();

    if (MaintenanceSlots == null)
        MaintenanceSlots = new List<MaintenanceSlots>();

    // Verifica conflitos com manutenção
    var maintenanceToday = MaintenanceSlots.FirstOrDefault(slot => slot.Date == date);
    if (maintenanceToday != null && maintenanceToday.TimeSlots.Any(ts =>
        ts.StartMinute < endMinute && ts.EndMinute > startMinute))
    {
        return false; // Sala está em manutenção durante o período solicitado
    }

    // Verifica conflitos com agendamentos existentes
    foreach (var app in Appointments)
    {
        var appDate = app.AppointmentTimeSlot.Date;
        var appStart = app.AppointmentTimeSlot.TimeSlot.StartMinute;
        var appEnd = app.AppointmentTimeSlot.TimeSlot.EndMinute;

        // Só verifica se a data coincide
        if (appDate == date)
        {
            // Casos de sobreposição:
            if (
                (startMinute < appStart && endMinute > appEnd) || // Começa antes e termina depois
                (startMinute < appStart && endMinute > appStart && endMinute <= appEnd) || // Começa antes e termina antes
                (startMinute < appStart && endMinute == appEnd) || // Começa antes e termina ao mesmo tempo
                (startMinute == appStart && endMinute > appEnd) || // Começa ao mesmo tempo e termina depois
                (startMinute == appStart && endMinute < appEnd) || // Começa ao mesmo tempo e termina antes
                (startMinute == appStart && endMinute == appEnd) || // Começa ao mesmo tempo e termina ao mesmo tempo
                (startMinute == appEnd) || // Começa exatamente no fim do outro
                (endMinute > appStart && startMinute < appStart) // Termina depois do início do outro
            )
            {
                return false; // Sala está ocupada
            }
        }
    }

    // Nenhum conflito encontrado
    return true;
}


    }
}
