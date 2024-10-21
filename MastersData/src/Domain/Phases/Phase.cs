using System;
using System.Collections.Generic;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.Specializations;
using DDDSample1.Domain.StaffMembers;

namespace DDDSample1.Domain.OperationTypes
{
    public class Phase : Entity<PhasesId>
    {
        public int duration { get; private set; }

        public List<RequiredStaff> requiredStaff { get; private set; }

        private Phase() { }

        public Phase(int duration, List<RequiredStaff> requiredStaff)
        {
            if (duration <= 0)
            {
                throw new ArgumentException("Duration must be greater than 0.");
            }

            if (requiredStaff == null || requiredStaff.Count == 0)
            {
                throw new ArgumentNullException(nameof(requiredStaff), "Required staff cannot be null or empty.");
            }

            this.Id = new PhasesId(Guid.NewGuid());
            this.duration = duration;
            this.requiredStaff = requiredStaff;
        }

        public Phase(Guid id, int duration, List<RequiredStaff> requiredStaff) : this(duration, requiredStaff)
        {
            this.Id = new PhasesId(id);
        }




    }
}