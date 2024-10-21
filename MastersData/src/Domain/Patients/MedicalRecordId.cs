using System;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.Patients
{
    public class MedicalRecordId : EntityId
    {
        public MedicalRecordId(Guid value) : base(value)
        {

        }
        public Guid AsGuid()
        {
            return (Guid)base.ObjValue;
        }

        public override string AsString()
        {
            Guid obj = (Guid) base.ObjValue;
            return obj.ToString();
        }

        protected override object createFromString(string text)
        {
            return (Guid) base.ObjValue;
        }
    }
}