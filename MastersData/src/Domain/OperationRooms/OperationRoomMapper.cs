using System.Collections.Generic;
using DDDSample1.Domain.Appointments;
using DDDSample1.Domain.OperationRooms;
using DDDSample1.Domain.RoomTypes;
using DDDSample1.Domain.Shared;


namespace DDDSample1.Domain.OperationRooms
{
    public class OperationRoomMapper
    {
        public static OperationRoomDto ToDTO(OperationRoom operationRoom)
        {

            // Garantir que as listas não sejam nulas antes de ConvertAll
            RoomTypeDto roomType = operationRoom.RoomType != null ? RoomTypeMapper.toDTO(operationRoom.RoomType) : null;
            
            List<MaintenanceSlotsDto> maintenanceSlots = operationRoom.MaintenanceSlots?
                .ConvertAll(MaintenanceSlotMapper.ToDto) ?? new List<MaintenanceSlotsDto>();

            List<AppointmentDto> appointments = operationRoom.Appointments?
                .ConvertAll(AppointmentMapper.ToDto) ?? new List<AppointmentDto>();

            return new OperationRoomDto(
                operationRoom.Id.AsGuid(),
                operationRoom.RoomNumber.roomNumber,
                roomType,
                operationRoom.RoomCapacity.roomCapacity,
                operationRoom.RoomStatus.ToString(),
                maintenanceSlots,
                appointments
            );
        }


    }
}
