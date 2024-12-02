using System;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.Utils;

namespace DDDSample1.Domain.RoomTypes
{
    public class RoomType : Entity<RoomTypeId>, IAggregateRoot
    {
        public FullName Name { get; private set; }

        private RoomType() { }

        public RoomType(string name)
        {
            this.Id = new RoomTypeId(Guid.NewGuid());
            this.Name = new FullName(name);
        }
    }
}
