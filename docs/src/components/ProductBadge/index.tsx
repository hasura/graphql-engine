import React from 'react';
import VersionedLink from '@site/src/components/VersionedLink';

// Props, all bool
type ProductBadgeProps = {
  ce: boolean;
  free: boolean;
  standard: boolean;
  pro: boolean;
  ee: boolean;
  self: boolean;
};

// Create links based on props, we're adding a comma between each product
function createLinks(props: ProductBadgeProps) {
  const links: any[] = [];
  if (props.ce) {
    links.push(
      <VersionedLink key="ce" to="/getting-started/overview">
        Community Edition
      </VersionedLink>
    );
  }
  if (props.free) {
    links.push(
      <VersionedLink key="free" to="/hasura-cloud/overview/#cloud-free">
        Cloud Free
      </VersionedLink>
    );
  }
  if (props.standard) {
    links.push(
      <VersionedLink key="standard" to="/hasura-cloud/overview/#cloud-professional">
        Cloud Standard
      </VersionedLink>
    );
  }
  if (props.pro) {
    links.push(
      <VersionedLink key="pro" to="/hasura-cloud/overview/#cloud-professional">
        Cloud Professional
      </VersionedLink>
    );
  }
  if (props.ee) {
    links.push(
      <VersionedLink key="ee" to="/hasura-cloud/overview/#cloud-enterprise">
        Cloud Enterprise
      </VersionedLink>
    );
  }
  if (props.self) {
    links.push(
      <VersionedLink key="self" to="/enterprise/overview">
        Self-Hosted Enterprise
      </VersionedLink>
    );
  }
  return links.map((link, index) => {
    if (index === links.length - 1) {
      return link;
    }
    return [link, ', '];
  });
}

const ProductBadge = (props: ProductBadgeProps) => (
  <div className="badge badge--primary heading-badge">Available on: {createLinks(props)}</div>
);

export default ProductBadge;
