using System;
using System.Collections.Generic;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.AvailabilitySlots;
using DDDSample1.Domain.StaffMembers;
using System.Linq;
using DDDSample1.Domain.Utils;


namespace DDDSample1.Domain.AvailabilitySlots
{
    public class AvailabilitySlot : Entity<AvailabilitySlotsId>, IAggregateRoot
    {
        public String StaffId { get; private set; } // Referência ao Staff
        public List<DailyAvailability> Availability { get; private set; }



        private AvailabilitySlot() { }

        public AvailabilitySlot(String staffId)
        {
            this.Id = new AvailabilitySlotsId(Guid.NewGuid());
            this.StaffId = staffId;
            this.Availability = new List<DailyAvailability>();
        }

        public AvailabilitySlot(String staffId, List<DailyAvailability> availability)
        {
            this.Id = new AvailabilitySlotsId(Guid.NewGuid());
            this.StaffId = staffId;
            this.Availability = availability;
        }



        public void AddAvailability(DateOnly date, int startMinute, int endMinute)
        {
            TimeSlot timeSlot = new TimeSlot(startMinute, endMinute);

            var dailyAvailability = this.Availability.FirstOrDefault(avail => avail.Date == date);
            if (dailyAvailability == null)
            {
                dailyAvailability = new DailyAvailability(date);
                dailyAvailability.AddTimeSlot(startMinute, endMinute);
                this.Availability.Add(dailyAvailability);
            }
            else
            {
                dailyAvailability.AddTimeSlot(startMinute, endMinute);
            }
        }


        public bool IsAvailable(DateOnly date, int startMinute, int endMinute)
        {
            // Inicializa as listas se forem nulas
            if (startMinute < 0 || endMinute > 1440)
            {
                return false;
            }

            Console.WriteLine("Verificando disponibilidade para a data: " + date);
            if (Availability == null)
                return false;


            var dailyAvailability = this.Availability.FirstOrDefault(avail => avail.Date == date);

            if (dailyAvailability == null)
            {
                // Se não houver disponibilidade registrada para a data, retorna falso
                return false;
            }


            // Verifica conflitos com agendamentos existentes
            foreach (var slot in dailyAvailability.TimeSlots)
            {
                var slotStart = slot.StartMinute;
                Console.WriteLine("slotStart: " + slotStart);
                var slotEnd = slot.EndMinute;
                Console.WriteLine("slotEnd: " + slotEnd);

                // Casos de sobreposição:
                if ((startMinute >= slotStart && startMinute < slotEnd) ||
                    (endMinute >= slotStart && endMinute <= slotEnd) ||
                    (startMinute <= slotStart && endMinute >= slotEnd))
                {
                    return false;
                }
            }

            // Nenhum conflito encontrado
            return true;
        }
    }


}

