using System;
using System.Collections.Generic;
using DDDSample1.Domain.OperationRooms;
using DDDSample1.Domain.Utils;


namespace DDDSample1.Domain.OperationRooms
{

    public class MaintenanceSlotsDto
    {
        public DateOnly Date { get; set; }
        public List<TimeSlotDto> TimeSlots { get; set; } // Assumindo que TimeSlot é outro DTO

        public MaintenanceSlotsDto(DateOnly date, List<TimeSlotDto> timeSlots)
        {
            Date = date;
            TimeSlots = timeSlots;
        }
    }

}
