namespace DDDSample1.Domain.RoomTypes
{
    public class RoomTypeMapper
    {
        public static RoomTypeDto toDTO(RoomType roomType)
        {
            return new RoomTypeDto(roomType.Id.AsGuid(), roomType.Name.ToString());
        }

        public static RoomType toDomain(RoomTypeDto roomTypeDto)
        {
            return new RoomType(roomTypeDto.Name);
        }
    }
}