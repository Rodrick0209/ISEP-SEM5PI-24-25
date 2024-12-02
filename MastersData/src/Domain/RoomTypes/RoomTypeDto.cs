using System;

namespace DDDSample1.Domain.RoomTypes
{
    public class RoomTypeDto
    {
        public Guid Id { get; set; }
        public string Name { get; set; }

        public RoomTypeDto(Guid id, string name)
        {
            this.Id = id;
            this.Name = name;
        }
    }
}