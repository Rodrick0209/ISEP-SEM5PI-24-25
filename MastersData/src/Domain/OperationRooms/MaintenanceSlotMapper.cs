using DDDSample1.Domain.OperationRooms;
using System;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.Utils;


public static class MaintenanceSlotMapper
{
    public static MaintenanceSlotsDto ToDto(MaintenanceSlots maintenanceSlot)
    {

        return new MaintenanceSlotsDto(maintenanceSlot.Date, maintenanceSlot.TimeSlots.ConvertAll(slot => new TimeSlotDto(slot.StartMinute, slot.EndMinute)));
    }
}
