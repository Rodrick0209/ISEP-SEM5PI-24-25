using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;
using DDDSample1.Domain.AvailabilitySlots;


namespace DDDSample1.Infrastructure.AvailabilitySlots
{


    internal class AvailabilitySlotEntityTypeConfiguration : IEntityTypeConfiguration<AvailabilitySlot>
    {
        public void Configure(EntityTypeBuilder<AvailabilitySlot> builder)
        {
            builder.HasKey(b => b.Id);

            // Configure the Name property
            builder.Property(b => b.Name)
                   .IsRequired() // Make the Name property required
                   .HasMaxLength(100); // Set a maximum length for the Name property
        }
    }


}