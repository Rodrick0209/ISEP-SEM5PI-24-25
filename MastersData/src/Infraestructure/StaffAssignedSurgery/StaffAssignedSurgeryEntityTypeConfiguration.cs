using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;
using DDDSample1.Domain.OperationRequest;



namespace DDDSample1.Infrastructure.OperationRequests
{

    internal class StaffAssignedSurgeryEntityTypeConfiguration : IEntityTypeConfiguration<StaffAssignedSurgery>
    {

        public void Configure(EntityTypeBuilder<StaffAssignedSurgery> builder)
        {
        }
    }
}