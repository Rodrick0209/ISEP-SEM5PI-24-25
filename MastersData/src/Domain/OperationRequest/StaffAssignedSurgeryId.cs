using System;
using DDDSample1.Domain.Shared;
using Newtonsoft.Json;



namespace DDDSample1.Domain.OperationRequest
{

    public class StaffAssignedSurgeryId : EntityId
    {

        [JsonConstructor]
        public StaffAssignedSurgeryId(Guid value) : base(value)
        {

        }

        public StaffAssignedSurgeryId(String value) : base(value)
        {

        }


        override
        protected  Object createFromString(String text){
            return new Guid(text);
        }
        
        override
        public String AsString(){
            Guid obj = (Guid) base.ObjValue;
            return obj.ToString();
        }
        public Guid AsGuid(){
            return (Guid) base.ObjValue;
        } 

    }






}