import { CommonModule } from '@angular/common';
import { Component, EventEmitter, Output } from '@angular/core';
import { FormsModule } from '@angular/forms';

@Component({
  selector: 'app-filter-staffs',
  standalone: true,
  imports: [FormsModule, CommonModule],
  templateUrl: './filter-staffs.component.html',
  styleUrl: './filter-staffs.component.css'
})
export class FilterStaffsComponent {
  filter = {
    fullName: '',
    licenseNumber: '',
    phoneNumber: '',
    email: '',
    specializationId: ''
  };

  @Output() filterChanged = new EventEmitter<{ fullName: string, licenseNumber: string, phoneNumber : string, email: string, specializationId: string }>();

  applyFilter(): void {
    this.filterChanged.emit({
      ...this.filter
    });
  }

  onSearch(event: Event): void {
    event.preventDefault();
    this.applyFilter();
  }
}
