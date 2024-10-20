using System;
using DDDSample1.Domain.Shared;
using Newtonsoft.Json;


namespace DDDSample1.Domain.OperationTypes
{

    public class PhasesId : EntityId
    {

        [JsonConstructor]
        public PhasesId(Guid value) : base(value)
        {

        }


        public PhasesId(String value) : base(value)
        {

        }

        override
        protected Object createFromString(String text)
        {
           return new Guid(text);
        }
        
        override
        public string AsString()
        {
            Guid obj = (Guid) base.ObjValue;
            return obj.ToString();
        }

        public Guid AsGuid()
        {
            return (Guid)base.ObjValue;
        }
    }






}