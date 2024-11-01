using System;
using System.Text.Json.Serialization;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.Patients
{
    public class MedicalHistoryId : EntityId
    {
        [JsonConstructor]
        public MedicalHistoryId(Guid value) : base(value)
        {
        }

        public MedicalHistoryId(String value) : base(value)
        {
        }

      //  public Guid Value => AsGuid(); // Propriedade de acesso público para EF


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