using System;
using System.Collections.Generic;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.Specializations;
using DDDSample1.Domain.StaffMembers;

namespace DDDSample1.Domain.OperationType
{
    public class RequiredStaff : IValueObject
    {
        public int num {get; private set;}
        public SpecializationId specialization {get; private set;}
        

        public RequiredStaff(int num, SpecializationId specialization)
        {
            this.num = num;
            this.specialization = specialization ?? throw new ArgumentNullException(nameof(specialization));
        }

        public RequiredStaff()
        {
            // Parameterless constructor for ORM or serialization purposes
        }

        
    }
}