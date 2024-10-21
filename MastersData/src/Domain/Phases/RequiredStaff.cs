using System;
using System.Collections.Generic;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.Specializations;
using DDDSample1.Domain.StaffMembers;

namespace DDDSample1.Domain.OperationTypes
{
    public class RequiredStaff : IValueObject
    {
        public int num { get; private set; }
        public SpecializationId specialization { get; private set; }


        public RequiredStaff(int num, SpecializationId specialization)
        {
            if (num <= 0)
            {
                throw new ArgumentException("Number of required staff must be greater than zero.", nameof(num));
            }
            this.num = num;
            this.specialization = specialization ?? throw new ArgumentNullException(nameof(specialization));
        }

        public RequiredStaff()
        {
            // Parameterless constructor for ORM or serialization purposes
        }

        public void ChangeNumber(int newNum)
        {
            if (newNum <= 0)
            {
                throw new ArgumentException("Number of required staff must be greater than zero.", nameof(newNum));
            }
            this.num = newNum;
        }

        public void ChangeSpecialization(SpecializationId newSpecialization)
        {
            this.specialization = newSpecialization ?? throw new ArgumentNullException(nameof(newSpecialization));
        }

    }
}