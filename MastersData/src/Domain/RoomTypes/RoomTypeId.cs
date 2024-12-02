using System;
using DDDSample1.Domain.Shared;
using Newtonsoft.Json;

namespace DDDSample1.Domain.RoomTypes
{
    public class RoomTypeId : EntityId
    {
        [JsonConstructor]
        public RoomTypeId(Guid value) : base(value)
        {
        }

        public RoomTypeId(string value) : base(value)
        {
        }

        // Propriedade comentada, caso seja necessária para frameworks como EF
        // public Guid Value => AsGuid();

        override
        protected Object createFromString(String text)
        {
            return new Guid(text);
        }

        override
        public String AsString()
        {
            Guid obj = (Guid) base.ObjValue;
            return obj.ToString();
        }

        public Guid AsGuid()
        {
            return (Guid) base.ObjValue;
        }
    }
}
