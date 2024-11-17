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
    name: '',
    licenseNumber: '',
    phoneNumber: '',
    email: '',
    specialization: ''
  };

  @Output() filterChanged = new EventEmitter<{ name: string, licenseNumber: string, phoneNumber : string, email: string, specialization: string }>();

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
