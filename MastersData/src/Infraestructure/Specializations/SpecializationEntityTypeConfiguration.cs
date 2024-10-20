using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;
using DDDSample1.Domain.Specializations;


namespace DDDSample1.Infrastructure.Specializations
{


    internal class SpecializationEntityTypeConfiguration : IEntityTypeConfiguration<Specialization>
    {
        public void Configure(EntityTypeBuilder<Specialization> builder)
        {
            builder.HasKey(b => b.Id);

            // Configure the Name property
            builder.Property(b => b.Name)
                   .IsRequired() // Make the Name property required
                   .HasMaxLength(100); // Set a maximum length for the Name property
        }
    }


}