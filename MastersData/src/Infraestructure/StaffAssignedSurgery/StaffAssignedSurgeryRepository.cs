using System;
using System.Threading.Tasks;
using DDDSample1.Domain.OperationRequest;
using DDDSample1.Infrastructure.Shared;
using Microsoft.EntityFrameworkCore;



namespace DDDSample1.Infrastructure.OperationRequests
{

    public class StaffAssignedSurgeryRepository : BaseRepository<StaffAssignedSurgery, StaffAssignedSurgeryId>,IStaffAssignedSurgeryRepository

    {
        private readonly DDDSample1DbContext context;

        public StaffAssignedSurgeryRepository(DDDSample1DbContext context):base(context.StaffAssignedSurgeries)
        {
            this.context = context;
        }







    }


}