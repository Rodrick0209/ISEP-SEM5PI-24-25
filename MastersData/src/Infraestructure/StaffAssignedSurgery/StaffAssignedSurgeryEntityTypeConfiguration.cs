using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;
using DDDSample1.Domain.OperationRequest;



namespace DDDSample1.Infrastructure.OperationRequests
{

    internal class StaffAssignedSurgeryEntityTypeConfiguration : IEntityTypeConfiguration<StaffAssignedSurgery>
    {
        public void Configure(EntityTypeBuilder<StaffAssignedSurgery> builder)
        {
            builder.HasKey(s => s.Id);

            builder.Property(s => s.Id)
            .HasConversion(
            id => id.Value,
            value => new StaffAssignedSurgeryId(value))
            .IsRequired();

            builder.HasIndex(s => s.Id)
            .IsUnique();

            builder.OwnsMany(s => s.staffAnesthesyPhase, a =>
            {
            a.WithOwner().HasForeignKey("StaffAssignedSurgeryId");
            a.Property<int>("Id");
            a.HasKey("Id");
            });

            builder.OwnsMany(s => s.staffSurgeryPhase, s =>
            {
            s.WithOwner().HasForeignKey("StaffAssignedSurgeryId");
            s.Property<int>("Id");
            s.HasKey("Id");
            });
        }
    }
}
