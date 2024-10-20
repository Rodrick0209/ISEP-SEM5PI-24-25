using System;
using DDDSample1.Domain.Shared;
using Newtonsoft.Json;


namespace DDDSample1.Domain.OperationType
{

    public class PhasesId : EntityId
    {

        public PhasesId(String value):base(value)
        {

        }

        override
        protected  Object createFromString(String text){
            return text;
        }
        override
        public String AsString(){
            return (String) base.Value;
        }

    }






}