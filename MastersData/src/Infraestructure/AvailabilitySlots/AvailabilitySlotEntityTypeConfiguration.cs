using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;
using DDDSample1.Domain.AvailabilitySlots;
using DDDSample1.Domain.StaffMembers;

namespace DDDSample1.Infrastructure.AvailabilitySlots
{
    internal class AvailabilitySlotEntityTypeConfiguration : IEntityTypeConfiguration<AvailabilitySlot>
    {
        public void Configure(EntityTypeBuilder<AvailabilitySlot> builder)
        {
            builder.HasKey(b => b.Id);
        }   
    }
}